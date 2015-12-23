from django.shortcuts import render
from django.utils import timezone
from .models import Post

def post_list(request):
	return render(request, 'blog/post_list.html', {})
	
def Net_present_value(request):
	return render(request, 'blog/Net_present_value.html', {})
	
def Annuity(request):
	return render(request, 'blog/Annuity.html', {})

def Bond(request):
	return render(request, 'blog/Bond.html', {})
	
def Stock(request):
	return render(request, 'blog/Stock.html', {})
	
def Portfolio(request):
	return render(request, 'blog/Portfolio.html', {})