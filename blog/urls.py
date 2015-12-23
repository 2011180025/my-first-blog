from django.conf.urls import patterns, url
from . import views

urlpatterns = [
	url(r'^$', 'blog.views.post_list', name='post_list'),
	url(r'^Net_present_value/$', 'blog.views.Net_present_value', name='Net_present_value'),
	url(r'^Annuity/$', 'blog.views.Annuity', name='Annuity'),
	url(r'^Bond/$', 'blog.views.Bond', name='Bond'),
	url(r'^Stock/$', 'blog.views.Stock', name='Stock'),
	url(r'^Portfolio/$', 'blog.views.Portfolio', name='Portfolio'),
]
